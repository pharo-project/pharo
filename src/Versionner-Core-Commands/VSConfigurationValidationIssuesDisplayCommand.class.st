"
Display errors found in a configuration validation.

"
Class {
	#name : #VSConfigurationValidationIssuesDisplayCommand,
	#superclass : #VSCommand,
	#category : 'Versionner-Core-Commands'
}

{ #category : #execute }
VSConfigurationValidationIssuesDisplayCommand >> execute [
	| list |

	list := ListModel new.
	list openWithSpec.
	list title: target messageText.
	list items: target issues.
]
