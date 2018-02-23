"
I'm a tree node representing a message send, at least one time, in the package under analysis having no implementor in this package and its dependencies. 
"
Class {
	#name : #DAMissingMethodNode,
	#superclass : #DAMethodDependencyNode,
	#category : #'Tool-DependencyAnalyser-UI-Nodes'
}

{ #category : #api }
DAMissingMethodNode >> buildChildren [
	^ self messageSendNodes 
		addAll: self methodImplementationNodes;
		yourself
]

{ #category : #api }
DAMissingMethodNode >> hasChildren [
	^ true
]

{ #category : #accessing }
DAMissingMethodNode >> icon [
	^ self iconNamed: #testNotRunIcon
]

{ #category : #displaying }
DAMissingMethodNode >> label [
	^ 'Missing method: #', self content
]

{ #category : #accessing }
DAMissingMethodNode >> messageSendAnalysis [
	^ self parentNode messageSendAnalysis
]

{ #category : #api }
DAMissingMethodNode >> messageSendNodes [
	^ self messageSendAnalysis rPackage methods
		select: [ :method | method sendsSelector: self content ]
		thenCollect: [ :method | 
			DAMessageSendNode new
				content: method;
				parentNode: self;
				yourself ]
]

{ #category : #api }
DAMissingMethodNode >> methodImplementationNodes [
	^ (self content implementors collect: 
		[ :method | 
		DAMethodImplementationNode new
			content: method;
			parentNode: self;
			yourself ])
		sorted: [ :a :b | a content package name < b content package name ]
]

{ #category : #private }
DAMissingMethodNode >> packageMethods [
	^ self messageSendAnalysis rPackage methods
]

{ #category : #'browse-nautilus' }
DAMissingMethodNode >> selectInBrowser [
	"Nothing to browse"
]
