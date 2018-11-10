"
Description
--------------------

I am a simple action taking as parameters an owner of a project and the project name and I open the project on github from that.	
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	organisation:		<aString>		The name of the owner of the project.
	projectName:			<aString>		The project name.

"
Class {
	#name : #IceGitHubOpenOnGithubAction,
	#superclass : #IceGitHubAction,
	#instVars : [
		'organisation',
		'projectName'
	],
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #'instance creation' }
IceGitHubOpenOnGithubAction class >> organisation: aString projectName: anotherString [
	^ self new
		organisation: aString;
		projectName: anotherString;
		yourself
]

{ #category : #executing }
IceGitHubOpenOnGithubAction >> basicExecute [
	[ 
	
	| url |
	url := (self github getRepository: self organisation project: self projectName) at: 'html_url'.
	
	self class environment at: #WebBrowser ifPresent: [ :webBrowser | webBrowser openOn: url ] ifAbsent: [ self inform: ('Cannot open "{1}" because the project WebBrowser is not present by default in Pharo 6.' format: { url }) ] 
	
	]
		on: IceGitHubError
		do: [ self inform: ('Invalid Github repository. No project named "{1}" found for the owner "{2}"' format: {self projectName. self organisation}) ]
]

{ #category : #accessing }
IceGitHubOpenOnGithubAction >> organisation [
	^ organisation
]

{ #category : #accessing }
IceGitHubOpenOnGithubAction >> organisation: anObject [
	organisation := anObject
]

{ #category : #accessing }
IceGitHubOpenOnGithubAction >> projectName [
	^ projectName
]

{ #category : #accessing }
IceGitHubOpenOnGithubAction >> projectName: anObject [
	projectName := anObject
]
