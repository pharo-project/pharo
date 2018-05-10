"
I define the public API of compilers that can be used as system compilers (e.g. overriding #compiler on the class side).
"
Class {
	#name : #AbstractCompiler,
	#superclass : #Object,
	#category : #'OpalCompiler-Core-FrontEnd'
}

{ #category : #settings }
AbstractCompiler class >> compilerSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #compiler)
		label: 'Compiler';
		description: 'All Compiler Settings';
		with: [ (aBuilder pickOne: #compilerClass)
						target: SmalltalkImage;
						label: 'Default Compiler';
						domainValues: AbstractCompiler subclasses.
					(aBuilder group: #opalCompiler)
						label: 'Opal Compiler settings';
						with: [ OpalCompiler compilerSpecificSettingsOn: aBuilder ] ]
					
]

{ #category : #accessing }
AbstractCompiler >> class: aClass [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> compilationContext [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> compilationContext: anObject [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> compilationContextClass [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> compilationContextClass: aClass [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> compile [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> compiledMethodTrailer: aClass [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> context: aContext [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> decompileMethod: aCompiledMethod [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> encoderClass: aClass [ 
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> environment: anSmallTalkImage [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> evaluate [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> failBlock: aBlock [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> format [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> logged: aBoolean [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> noPattern: aBoolean [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> options: anArray [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> parse [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> parseLiterals: aString [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> parseSelector: aString [ 
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> receiver: anObject [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> requestor: aRequestor [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> requestorScopeClass: aClass [
	self subclassResponsibility
]

{ #category : #accessing }
AbstractCompiler >> source: aString [
	self subclassResponsibility
]

{ #category : #'public access' }
AbstractCompiler >> translate [
	self subclassResponsibility
]
