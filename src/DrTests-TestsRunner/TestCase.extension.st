Extension { #name : 'TestCase' }

{ #category : '*DrTests-TestsRunner' }
TestCase >> asResultForDrTest [
	^ DTTestLeafNode content: self
]

{ #category : '*DrTests-TestsRunner' }
TestCase >> drTestsBrowse [

	"May be using double dispatch would be better
	so that the application of DrTest can open tools 
	without relying on this global."
	
	(Smalltalk  tools toolNamed: #browser) 
		openOnClass: self class 
		selector: self selector
]

{ #category : '*DrTests-TestsRunner' }
TestCase >> drTestsBuildContextMenu: aMenuModel [
	"Nothing to display yet."
]

{ #category : '*DrTests-TestsRunner' }
TestCase >> drTestsName [
	^ self asString
]

{ #category : '*DrTests-TestsRunner' }
TestCase class >> drTestsSuite [
	^ self suite
]
