"
I reprensent a package assembly (i.e. a set of packages).
I collect all dependencies related to the assembly parts.
"
Class {
	#name : #DAAssemblyNode,
	#superclass : #DANode,
	#instVars : [
		'dependencies'
	],
	#category : #'Tool-DependencyAnalyser-UI-Nodes'
}

{ #category : #'instance creation' }
DAAssemblyNode class >> newWith: aCollection [ 
	^ self basicNew
		initializeWith: aCollection;
		yourself
]

{ #category : #api }
DAAssemblyNode >> childNodeClassFor: anItem [
	^ DADependentPackageNode
]

{ #category : #accessing }
DAAssemblyNode >> children [
	^ dependencies
]

{ #category : #accessing }
DAAssemblyNode >> daPackage [
	^ self parentNode daPackage
]

{ #category : #accessing }
DAAssemblyNode >> dependencies [
	^ dependencies
]

{ #category : #testing }
DAAssemblyNode >> hasChildren [
	^ true
]

{ #category : #accessing }
DAAssemblyNode >> icon [
	^ self iconNamed: #forwardIcon
]

{ #category : #testing }
DAAssemblyNode >> includes: aPackageName [
	^ (self dependencies collect: #packageName) includes: aPackageName
]

{ #category : #initialization }
DAAssemblyNode >> initializeWith: aCollection [ 
	self initialize.
	dependencies := aCollection
]

{ #category : #displaying }
DAAssemblyNode >> label [
	^ self content
]

{ #category : #'browse-nautilus' }
DAAssemblyNode >> selectInBrowser [
	"nothing to browse"
]
