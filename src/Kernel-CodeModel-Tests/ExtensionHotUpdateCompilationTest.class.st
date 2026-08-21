Class {
	#name : 'ExtensionHotUpdateCompilationTest',
	#superclass : 'AbstractExtensionTest',
	#instVars : [
		'objectClass',
		'pointClass',
		'coloredPointClass',
		'stringClass',
		'nicerStringClass',
		'userClass',
		'extensionOnObject',
		'extensionOnPoint',
		'extensionOnColoredPoint'
	],
	#category : 'Kernel-CodeModel-Tests-Extensions',
	#package : 'Kernel-CodeModel-Tests',
	#tag : 'Extensions'
}

{ #category : 'asserting' }
ExtensionHotUpdateCompilationTest >> assertDNU: scopedSelector on: selector to: receiver [
	
	
	self should: [ receiver perform: selector ] raise: MessageNotUnderstood withExceptionDo: [ :ex |
		self assert: ex receiver == receiver.
		self assert: ex message selector == scopedSelector. ]
]

{ #category : 'running' }
ExtensionHotUpdateCompilationTest >> setUp [

	super setUp.

	objectClass := self createClassNamed: #MyObject superclass: isolatingRoot package: userPackage.
	pointClass := self createClassNamed: #MyPoint superclass: objectClass package: userPackage.
	coloredPointClass := self createClassNamed: #MyColoredPoint superclass: pointClass package: userPackage.
	
	"String will have no extensions. It's here just for polymorphism.
	It will thus contain aliases only"
	stringClass := self createClassNamed: #MyString superclass: objectClass package: userPackage.
	nicerStringClass := self createClassNamed: #MyNicerString superclass: stringClass package: userPackage.
	userClass := self createClassNamed: #MyUserClass superclass: objectClass package: userPackage.

	extensionOnObject := self createExtensionNamed: #ExtensionOfObject forBehavior: objectClass inPackage: definerPackage.
	extensionOnPoint := self createExtensionNamed: #ExtensionOfPoint forBehavior: pointClass inPackage: definerPackage.
	extensionOnColoredPoint := self
		                           createExtensionNamed: #ExtensionOfColoredPoint
		                           forBehavior: coloredPointClass
		                           inPackage: definerPackage
]

{ #category : 'tests' }
ExtensionHotUpdateCompilationTest >> test01CompileNewExtensionMethod01IsVisibleForNewMethods [
	"A normal method sends the (non scoped) extension message"

	| classInDefinerPackage |
	classInDefinerPackage := self createClassNamed: #ClassInDefinerPackage superclass: objectClass package: definerPackage.
	
	"Now we define an extension in the scope.
	=> the method above (foo) should see the extension!"
	self compile: 'extension ^ 11' on: extensionOnObject.
	self compile: 'foo ^ self extension' on: classInDefinerPackage.
	self assert: classInDefinerPackage new foo equals: 11
]

{ #category : 'tests' }
ExtensionHotUpdateCompilationTest >> test01CompileNewExtensionMethod02RecompilesSendersInScope [
	"A normal method sends the (non scoped) extension message"

	| classInDefinerPackage |
	classInDefinerPackage := self createClassNamed: #ClassInDefinerPackage superclass: objectClass package: definerPackage.

	self compile: 'foo ^ self extension' on: classInDefinerPackage.
	
	"Before compiling the extension, we do not see a scoped selector. We DNU on the global selector"
	self assertDNU: #extension on: #foo to: classInDefinerPackage new.
	
	"Now we define an extension in the scope.
	=> the method above (foo) should see the extension!"
	self compile: 'extension ^ 11' on: extensionOnObject.

	self assert: classInDefinerPackage new foo equals: 11
]

{ #category : 'tests' }
ExtensionHotUpdateCompilationTest >> test02ImportNewExtensionMethod01IsVisibleForNewMethods [
	"A normal method sends the (non scoped) extension message"

	| scopedSelector |
	"Now we import an extension in the scope.
	=> the method above (foo) should see the extension!"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnObject.

	userClass package addExtensionImport: extensionOnObject.
	self compile: 'foo ^ self extension' on: userClass.
	self assert: userClass new foo equals: 11
]

{ #category : 'tests' }
ExtensionHotUpdateCompilationTest >> test02ImportNewExtensionMethod02RecompilesSendersInScope [
	"A normal method sends the (non scoped) extension message"

	| scopedSelector |
	self compile: 'foo ^ self extension' on: userClass. "Now we import an extension in the scope.
	=> the method above (foo) should see the extension!"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnObject. "Before importing the extension, we do not see a scoped selector. We DNU on the global selector"
	self assertDNU: #extension on: #foo to: userClass new.

	userClass package addExtensionImport: extensionOnObject.

	self assert: userClass new foo equals: 11
]

{ #category : 'tests' }
ExtensionHotUpdateCompilationTest >> test03CompileNewExtensionMethodAfterImport01IsVisibleForNewMethods [
	"A normal method sends the (non scoped) extension message"

	| scopedSelector |
	"We import an extension in the scope. The extension is empty"
	userClass package addExtensionImport: extensionOnObject. "=> the method above (foo) should see the extension!"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnObject.
	self compile: 'foo ^ self extension' on: userClass.
	self assert: userClass new foo equals: 11
]

{ #category : 'tests' }
ExtensionHotUpdateCompilationTest >> test03CompileNewExtensionMethodAfterImport02RecompilesSendersInScope [
	"A normal method sends the (non scoped) extension message"

	| scopedSelector |
	"We import an extension in the scope. The extension is empty"
	userClass package addExtensionImport: extensionOnObject.
	self compile: 'foo ^ self extension' on: userClass. "=> the method above (foo) should see the extension!"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnObject.
	self assert: userClass new foo equals: 11
]

{ #category : 'tests' }
ExtensionHotUpdateCompilationTest >> test04ReCompileNewExtensionMethod01IsVisibleForNewMethods [
	"A normal method sends the (non scoped) extension message"

	| classInDefinerPackage |
	classInDefinerPackage := self createClassNamed: #ClassInDefinerPackage superclass: objectClass package: definerPackage.
	
	"Now we define an extension in the scope.
	=> the method above (foo) should see the extension!"
	self compile: 'extension ^ 11' on: extensionOnObject.
	
	"Recompilation should not change anything"
	self compile: 'extension ^ 11' on: extensionOnObject.
	
	self compile: 'foo ^ self extension' on: classInDefinerPackage.
	
	self assert: classInDefinerPackage new foo equals: 11
]

{ #category : 'tests' }
ExtensionHotUpdateCompilationTest >> test04ReCompileNewExtensionMethod02RecompilesSendersInScope [
	"A normal method sends the (non scoped) extension message"

	| classInDefinerPackage |
	classInDefinerPackage := self createClassNamed: #ClassInDefinerPackage superclass: objectClass package: definerPackage.
	
	self compile: 'foo ^ self extension' on: classInDefinerPackage.
	
	"Before compiling the extension, we do not see a scoped selector. We DNU on the global selector"
	self assertDNU: #extension on: #foo to: classInDefinerPackage new.
	
	"Now we define an extension in the scope.
	=> the method above (foo) should see the extension!"
	self compile: 'extension ^ 11' on: extensionOnObject.
	
	"Recompilation should not change anything"
	self compile: 'extension ^ 11' on: extensionOnObject.

	self assert: classInDefinerPackage new foo equals: 11
]

{ #category : 'tests' }
ExtensionHotUpdateCompilationTest >> test05ReCompileNewExtensionMethodAfterImport01IsVisibleForNewMethods [
	"A normal method sends the (non scoped) extension message"

	| scopedSelector |
	"We import an extension in the scope. The extension is empty"
	userClass package addExtensionImport: extensionOnObject. "=> the method above (foo) should see the extension!"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnObject.
	self compile: 'foo ^ self extension' on: userClass. "Recompilation should not change anything"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnObject.
	self assert: userClass new foo equals: 11
]

{ #category : 'tests' }
ExtensionHotUpdateCompilationTest >> test05ReCompileNewExtensionMethodAfterImport02RecompilesSendersInScope [
	"A normal method sends the (non scoped) extension message"

	| scopedSelector |
	"We import an extension in the scope. The extension is empty"
	userClass package addExtensionImport: extensionOnObject.
	self compile: 'foo ^ self extension' on: userClass. "=> the method above (foo) should see the extension!"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnObject. "Recompilation should not change anything"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnObject.
	self assert: userClass new foo equals: 11
]

{ #category : 'tests' }
ExtensionHotUpdateCompilationTest >> test06RedefineExtensionMethod01IsVisibleForNewMethods [
	"A normal method sends the (non scoped) extension message"

	| classInDefinerPackage |
	classInDefinerPackage := self createClassNamed: #ClassInDefinerPackage superclass: objectClass package: definerPackage.
	
	"Now we define an extension in the scope.
	=> the method above (foo) should see the extension!"
	self compile: 'extension ^ 11' on: extensionOnObject.
	
	"We redefine it, and this new version should be taken into account"
	self compile: 'extension ^ 12' on: extensionOnObject.
	
	self compile: 'foo ^ self extension' on: classInDefinerPackage.
	
	self assert: classInDefinerPackage new foo equals: 12
]

{ #category : 'tests' }
ExtensionHotUpdateCompilationTest >> test06RedefineExtensionMethod02RecompilesSendersInScope [
	"A normal method sends the (non scoped) extension message"

	| classInDefinerPackage |
	classInDefinerPackage := self createClassNamed: #ClassInDefinerPackage superclass: objectClass package: definerPackage.
	
	self compile: 'foo ^ self extension' on: classInDefinerPackage.
	
	"Before compiling the extension, we do not see a scoped selector. We DNU on the global selector"
	self assertDNU: #extension on: #foo to: classInDefinerPackage new.
	
	"Now we define an extension in the scope.
	=> the method above (foo) should see the extension!"
	self compile: 'extension ^ 11' on: extensionOnObject.
	
	"We redefine it, and this new version should be taken into account"
	self compile: 'extension ^ 12' on: extensionOnObject.

	self assert: classInDefinerPackage new foo equals: 12
]

{ #category : 'tests' }
ExtensionHotUpdateCompilationTest >> test07RedefineExtensionMethodAfterImport01IsVisibleForNewMethods [
	"A normal method sends the (non scoped) extension message"

	| scopedSelector |
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnObject. "We import an extension in the scope. The extension is empty"
	userClass package addExtensionImport: extensionOnObject. "We redefine it, and this new version should be taken into account"
	self compile: 'extension ^ 12' on: extensionOnObject.
	self compile: 'foo ^ self extension' on: userClass.
	self assert: userClass new foo equals: 12
]

{ #category : 'tests' }
ExtensionHotUpdateCompilationTest >> test07RedefineExtensionMethodAfterImport02RecompilesSendersInScope [
	"A normal method sends the (non scoped) extension message"

	| scopedSelector |
	"=> the method above (foo) should see the extension!"
	scopedSelector := self compile: 'extension ^ 11' on: extensionOnObject. "We import an extension in the scope. The extension is empty"
	userClass package addExtensionImport: extensionOnObject.
	self compile: 'foo ^ self extension' on: userClass.
	self compile: 'extension ^ 12' on: extensionOnObject.

	self assert: userClass new foo equals: 12
]
