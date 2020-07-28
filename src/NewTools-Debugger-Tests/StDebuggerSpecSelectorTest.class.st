Class {
	#name : #StDebuggerSpecSelectorTest,
	#superclass : #TestCase,
	#instVars : [
		'specSelector'
	],
	#category : #'NewTools-Debugger-Tests-Model'
}

{ #category : #helpers }
StDebuggerSpecSelectorTest >> defaultSelectorWithExtensions [
	^ StDebuggerSpecSelector
		defaultDebuggerWithExtensionSpecSelector
]

{ #category : #helpers }
StDebuggerSpecSelectorTest >> defaultSelectorWithoutExtensions [
	^ StDebuggerSpecSelector
		defaultDebuggerWithoutExtensionSpecSelector
]

{ #category : #running }
StDebuggerSpecSelectorTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	super setUp.
	specSelector := StDebuggerSpecSelector new
]

{ #category : #'tests - spec' }
StDebuggerSpecSelectorTest >> testCustomSelectExtensionActivationSpecFrom [
	| customSelector mockSelector |
	customSelector := #anySelector.
	mockSelector := StDebuggerUsingSpecSelectorMock new.
	
	mockSelector initializeWithExtensionsNoneActivated.
	self
		assert: (mockSelector selectExtensionActivationSpecFrom: customSelector)
		equals: self defaultSelectorWithoutExtensions.
		
	mockSelector initializeWithoutExtensionsNoneActivated.
	self
		assert: (mockSelector selectExtensionActivationSpecFrom: customSelector)
		equals: self defaultSelectorWithoutExtensions.
		
	mockSelector initializeWithoutExtensionsAllActivated.
	self
		assert: (mockSelector selectExtensionActivationSpecFrom: customSelector)
		equals: self defaultSelectorWithoutExtensions.
		
	mockSelector initializeWithExtensionsAllActivated.		
	self
		assert: (mockSelector selectExtensionActivationSpecFrom: customSelector)
		equals: self defaultSelectorWithExtensions
]

{ #category : #'tests - spec' }
StDebuggerSpecSelectorTest >> testCustomSelectSpecSelector [
	| customSelector mockSelector |
	mockSelector := StDebuggerUsingSpecSelectorMock new.
	customSelector := mockSelector nonDefaultSelector.	
	mockSelector specSelector: customSelector.
	
	mockSelector initializeWithExtensionsNoneActivated.
	self assert: mockSelector selectSpecSelector equals: customSelector.
	
	mockSelector initializeWithoutExtensionsNoneActivated.
	self assert: mockSelector selectSpecSelector equals: customSelector.
	
	mockSelector initializeWithoutExtensionsAllActivated.
	self assert: mockSelector selectSpecSelector equals: customSelector.
	
	mockSelector initializeWithExtensionsAllActivated.	
	self assert: mockSelector selectSpecSelector equals: customSelector
]

{ #category : #'tests - spec' }
StDebuggerSpecSelectorTest >> testDebuggerClass [
	self assert: specSelector debuggerClass identicalTo: StDebugger
]

{ #category : #'tests - spec' }
StDebuggerSpecSelectorTest >> testDefaultSelector [
	self
		assert: StDebuggerSpecSelector defaultSelector specSelector
		equals: specSelector class defaultDebuggerWithExtensionSpecSelector
]

{ #category : #'tests - spec' }
StDebuggerSpecSelectorTest >> testIsDefaultSelector [
	self deny: (specSelector isDefaultSelector: #anySelector).
	self
		assert:
			(specSelector
				isDefaultSelector: specSelector class defaultDebuggerWithExtensionSpecSelector).
	self
		assert:
			(specSelector
				isDefaultSelector: specSelector class defaultDebuggerWithoutExtensionSpecSelector)
]

{ #category : #'tests - spec' }
StDebuggerSpecSelectorTest >> testNilSelectSpecSelector [
	| withExtensionSelector withoutExtensionSelector  mockSelector |
	withExtensionSelector := self defaultSelectorWithExtensions.
	withoutExtensionSelector := self defaultSelectorWithoutExtensions.	
	mockSelector := StDebuggerUsingSpecSelectorMock new.
	mockSelector specSelector: nil.
	
	mockSelector	initializeWithExtensionsNoneActivated.	
	self assert: mockSelector selectSpecSelector equals: withoutExtensionSelector.
	
	mockSelector initializeWithoutExtensionsNoneActivated.	
	self assert: mockSelector selectSpecSelector equals: withoutExtensionSelector.
	
	mockSelector initializeWithoutExtensionsAllActivated.
	self assert: mockSelector selectSpecSelector equals: withoutExtensionSelector.
	
	mockSelector initializeWithExtensionsAllActivated.		
	self assert: mockSelector selectSpecSelector equals: withExtensionSelector
]

{ #category : #'tests - spec' }
StDebuggerSpecSelectorTest >> testSelectExtensionActivationSpecFrom [
	| withExtensionSelector withoutExtensionSelector mockSelector |
	withExtensionSelector := self defaultSelectorWithExtensions.
	withoutExtensionSelector := self defaultSelectorWithoutExtensions.	
	mockSelector := StDebuggerUsingSpecSelectorMock new.
		
	mockSelector	initializeWithExtensionsNoneActivated.		
	self
		assert: (mockSelector selectExtensionActivationSpecFrom: withExtensionSelector)
		equals: withoutExtensionSelector.
	self
		assert: (mockSelector selectExtensionActivationSpecFrom: withoutExtensionSelector)
		equals: withoutExtensionSelector.
		
	mockSelector initializeWithoutExtensionsNoneActivated.
	self
		assert: (mockSelector selectExtensionActivationSpecFrom: withExtensionSelector)
		equals: withoutExtensionSelector.
	self
		assert: (mockSelector selectExtensionActivationSpecFrom: withoutExtensionSelector)
		equals: withoutExtensionSelector.
		
	mockSelector initializeWithoutExtensionsAllActivated.
	self
		assert: (mockSelector selectExtensionActivationSpecFrom: withExtensionSelector)
		equals: withoutExtensionSelector.
	self
		assert: (mockSelector selectExtensionActivationSpecFrom: withoutExtensionSelector)
		equals: withoutExtensionSelector.
		
	mockSelector initializeWithExtensionsAllActivated.
	self
		assert: (mockSelector selectExtensionActivationSpecFrom: withExtensionSelector)
		equals: withExtensionSelector.
	self
		assert: (mockSelector selectExtensionActivationSpecFrom: withoutExtensionSelector)
		equals: withoutExtensionSelector.
]

{ #category : #'tests - spec' }
StDebuggerSpecSelectorTest >> testSelectNonDefaultSpecSelector [
	| nonDefaultSelector mockSelector |		
	mockSelector := StDebuggerUsingSpecSelectorMock new.	
	nonDefaultSelector := mockSelector nonDefaultSelector.
	mockSelector specSelector: nonDefaultSelector.	
		
	mockSelector	initializeWithExtensionsNoneActivated.
	self
		assert: mockSelector selectSpecSelector
		equals: nonDefaultSelector.		
		
	mockSelector initializeWithoutExtensionsNoneActivated.
	self
		assert: mockSelector selectSpecSelector
		equals: nonDefaultSelector.
				
	mockSelector initializeWithoutExtensionsAllActivated.
	self
		assert: mockSelector selectSpecSelector
		equals: nonDefaultSelector.
		
	mockSelector initializeWithExtensionsAllActivated.	
	self
		assert: mockSelector selectSpecSelector
		equals: nonDefaultSelector
	
]

{ #category : #'tests - spec' }
StDebuggerSpecSelectorTest >> testSelectSpecSelectorWithExtensions [
	| withExtensionSelector withoutExtensionSelector mockSelector |
	withExtensionSelector := self defaultSelectorWithExtensions.
	withoutExtensionSelector := self defaultSelectorWithoutExtensions.			
	mockSelector := StDebuggerUsingSpecSelectorMock new.
	mockSelector specSelector: withExtensionSelector.	
			
	mockSelector initializeWithExtensionsNoneActivated.
	self
		assert: mockSelector selectSpecSelector
		equals: withoutExtensionSelector.	
		
	mockSelector initializeWithoutExtensionsNoneActivated.
	self
		assert: mockSelector selectSpecSelector
		equals: withoutExtensionSelector.
		
	mockSelector initializeWithoutExtensionsAllActivated.
	self
		assert: mockSelector selectSpecSelector
		equals: withoutExtensionSelector.
		
	mockSelector initializeWithExtensionsAllActivated.
	self
		assert: mockSelector selectSpecSelector
		equals: withExtensionSelector
]

{ #category : #'tests - spec' }
StDebuggerSpecSelectorTest >> testSelectSpecSelectorWithoutExtensions [
	| withoutExtensionSelector mockSelector |
	withoutExtensionSelector := self defaultSelectorWithoutExtensions.			
	mockSelector := StDebuggerUsingSpecSelectorMock new.
	mockSelector specSelector: withoutExtensionSelector.	
		
	mockSelector initializeWithExtensionsNoneActivated.		
	self
		assert: mockSelector selectSpecSelector
		equals: withoutExtensionSelector.
		
	mockSelector initializeWithoutExtensionsNoneActivated.	
	self
		assert: mockSelector selectSpecSelector
		equals: withoutExtensionSelector.
		
	mockSelector initializeWithoutExtensionsAllActivated.	
	self
		assert: mockSelector selectSpecSelector
		equals: withoutExtensionSelector.
		
	mockSelector initializeWithExtensionsAllActivated.	
	self
		assert: mockSelector selectSpecSelector
		equals: withoutExtensionSelector.
]

{ #category : #'tests - spec' }
StDebuggerSpecSelectorTest >> testSelectorForDebuggerStatusExtensionsStatus [
	| selector |
	selector := specSelector
		selectorForDebuggerStatus: false
		extensionsStatus: false.
	self
		assert: selector
		equals: specSelector class defaultDebuggerWithoutExtensionSpecSelector.
		
	selector := specSelector
		selectorForDebuggerStatus: true
		extensionsStatus: false.
	self
		assert: selector
		equals: specSelector class defaultDebuggerWithoutExtensionSpecSelector.
		
	selector := specSelector
		selectorForDebuggerStatus: false
		extensionsStatus: true.
	self
		assert: selector
		equals: specSelector class defaultDebuggerWithoutExtensionSpecSelector.
		
	selector := specSelector
		selectorForDebuggerStatus: true
		extensionsStatus: true.
	self
		assert: selector
		equals: specSelector class defaultDebuggerWithExtensionSpecSelector
]
