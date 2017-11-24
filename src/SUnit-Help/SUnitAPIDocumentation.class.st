"
SUnit API documentation for help system
"
Class {
	#name : #SUnitAPIDocumentation,
	#superclass : #SUnitHelp,
	#category : #'SUnit-Help'
}

{ #category : #accessing }
SUnitAPIDocumentation class >> bookName [
	^'API Documentation'
]

{ #category : #defaults }
SUnitAPIDocumentation class >> builder [
	^PackageAPIHelpBuilder
]

{ #category : #accessing }
SUnitAPIDocumentation class >> helpPackages [
	^#('SUnit-Core-Announcements' 'SUnit-Core-Extensions' 'SUnit-Core-Kernel' 'SUnit-Core-Kernel' 'SUnit-Core-Utilities' 'SUnit-Tests' 'SUnit-UI')
]
