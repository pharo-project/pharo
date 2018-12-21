Class {
	#name : #OCSourceTemplates,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Source'
}

{ #category : #tests }
OCSourceTemplates >> testsourceCodeTemplateFor [
	"check for distictive source code templates for class-side and instance-side"
	self assert: ((Behavior sourceCodeTemplateFor: Object) includesSubstring: 'instance-side message'). 
	self assert: ((Behavior sourceCodeTemplateFor: Object class) includesSubstring: 'class-side message') 
	 
]
