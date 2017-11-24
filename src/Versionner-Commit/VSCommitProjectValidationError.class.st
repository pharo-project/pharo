"
I'm an error thrown whenever the project validation fails. 

I contain a project reference and a list of issues.
"
Class {
	#name : #VSCommitProjectValidationError,
	#superclass : #Error,
	#instVars : [
		'projectName',
		'issues'
	],
	#category : #'Versionner-Commit'
}

{ #category : #accessing }
VSCommitProjectValidationError >> issues [
	^ issues
]

{ #category : #accessing }
VSCommitProjectValidationError >> issues: anObject [
	issues := anObject
]

{ #category : #accessing }
VSCommitProjectValidationError >> projectName [
	^ projectName
]

{ #category : #accessing }
VSCommitProjectValidationError >> projectName: anObject [
	projectName := anObject
]
