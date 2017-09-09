package freestyle.plugin.action

import java.util.Properties

import com.intellij.ide.actions.{CreateFileFromTemplateDialog, CreateTemplateInPackageAction}
import com.intellij.ide.fileTemplates.{FileTemplate, FileTemplateManager, JavaTemplateUtil}
import com.intellij.openapi.project.{DumbAware, Project}
import com.intellij.openapi.ui.InputValidatorEx
import com.intellij.openapi.util.IconLoader
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi._
import com.intellij.psi.codeStyle.CodeStyleManager
import org.jetbrains.annotations.NonNls
import org.jetbrains.jps.model.java.JavaModuleSourceRootTypes
import org.jetbrains.plugins.scala.ScalaFileType
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil

/**
 * https://github.com/AdrianRaFo
 */
class CreationAction
    extends CreateTemplateInPackageAction[ScTypeDefinition](
      "New Freestyle Algebra",
      "Creates new Freestyle Algebra",
      IconLoader.getIcon("/images/logo.png"),
      JavaModuleSourceRootTypes.SOURCES)
    with DumbAware {

  protected def buildDialog(
      project: Project,
      directory: PsiDirectory,
      builder: CreateFileFromTemplateDialog.Builder) {
    builder.addKind("Algebra", null, "Freestyle Algebra")
    builder.addKind("Tagless", null, "Freestyle Tagless")
    builder.addKind("Module", null, "Freestyle Module")

    for (template <- FileTemplateManager.getInstance(project).getAllTemplates) {
      if (checkPackageExists(directory)) {
        builder.addKind(template.getName, null, template.getName)
      }
    }

    builder.setTitle("Create New Freestyle Algebra")
    builder.setValidator(new InputValidatorEx {
      def getErrorText(inputString: String): String = {
        if (inputString.length > 0 && !ScalaNamesUtil.isQualifiedName(inputString)) {
          return "This is not a valid Scala qualified name"
        }
        null
      }

      def checkInput(inputString: String): Boolean =
        true

      def canClose(inputString: String): Boolean =
        !StringUtil.isEmptyOrSpaces(inputString) && getErrorText(inputString) == null
    })
  }
  def getActionName(directory: PsiDirectory, newName: String, templateName: String): String =
    "New Freestyle Algebra"

  def getNavigationElement(createdElement: ScTypeDefinition): PsiElement =
    createdElement.extendsBlock

  def doCreate(directory: PsiDirectory, newName: String, templateName: String): ScTypeDefinition = {
    createClassFromTemplate(directory, newName, templateName) match {
      case scalaFile: ScalaFile =>
        scalaFile.typeDefinitions.headOption.orNull
    }
  }

  private def createClassFromTemplate(
      directory: PsiDirectory,
      className: String,
      templateName: String,
      parameters: String*): PsiFile =
    CreationAction.createFromTemplate(directory, className, templateName, parameters: _*)

}

object CreationAction {
  @NonNls private[action] val NAME_TEMPLATE_PROPERTY: String          = "NAME"
  @NonNls private[action] val LOW_CASE_NAME_TEMPLATE_PROPERTY: String = "lowCaseName"

  def createFromTemplate(
      directory: PsiDirectory,
      name: String,
      templateName: String,
      parameters: String*): PsiFile = {
    val project = directory.getProject
    val template: FileTemplate =
      FileTemplateManager.getInstance(project).getInternalTemplate(templateName)
    val properties: Properties = new Properties(
      FileTemplateManager.getInstance(project).getDefaultProperties())

    properties.setProperty(
      FileTemplate.ATTRIBUTE_PACKAGE_NAME,
      ScalaNamesUtil.escapeKeywordsFqn(JavaTemplateUtil.getPackageName(directory)))
      properties.setProperty(NAME_TEMPLATE_PROPERTY, name)
    properties.setProperty(
      LOW_CASE_NAME_TEMPLATE_PROPERTY,
      name.substring(0, 1).toLowerCase + name.substring(1))

    var i: Int = 0
    while (i < parameters.length) {
      {
        properties.setProperty(parameters(i), parameters(i + 1))
      }
      i += 2
    }
    var text: String = null
    try {
      text = template.getText(properties)
    } catch {
      case e: Exception =>
        throw new RuntimeException(
          "Unable to load template for " + FileTemplateManager.getDefaultInstance
            .internalTemplateToSubject(templateName),
          e)
    }
    val factory: PsiFileFactory = PsiFileFactory.getInstance(project)
    val scalaFileType           = ScalaFileType.INSTANCE
    var fileName = name
    if(templateName == "Freestyle Module")
      fileName="modules"
    val file: PsiFile =
      factory.createFileFromText(s"$fileName.${scalaFileType.getDefaultExtension}", scalaFileType, text)
    CodeStyleManager.getInstance(project).reformat(file)
    directory.add(file).asInstanceOf[PsiFile]
  }
}
