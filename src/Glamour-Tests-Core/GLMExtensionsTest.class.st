Class {
	#name : #GLMExtensionsTest,
	#superclass : #TestCase,
	#category : 'Glamour-Tests-Core'
}

{ #category : #tests }
GLMExtensionsTest >> testGlamourValue [
	"Block closure"

	self assert: ([ #foo ] glamourValue: #(1 2)) equals: #foo.
	self assert: ([ :first | Array with: first ] glamourValue: #(1 2)) equals: #(#(1 2)).
	self assert: ([ :first :second | Array with: first with: second ] glamourValue: #(1 2)) equals: #(#(1 2) nil).
	self
		assert: ([ :first :second | Array with: first with: second ] glamourValue: (GLMMultiValue with: 1 with: 2))
		equals: #(1 2).	"Symbol"
	self assert: (#yourself glamourValueWithArgs: #(1 2)) equals: 1.
	self assert: (#yourself glamourValueWithArgs: #()) equals: nil	"self assert: (#yourself glamourValue: (MultiValue with: 1 with: 2)) = 1."
]

{ #category : #tests }
GLMExtensionsTest >> testGlamourValueWithArgs [
	"Block closure"

	self assert: ([ #foo ] glamourValueWithArgs: #(1 2)) equals: #foo.
	self assert: ([ :first | Array with: first ] glamourValueWithArgs: #(1 2)) equals: #(1).
	self assert: ([ :first :second | Array with: first with: second ] glamourValueWithArgs: #(1 2)) equals: #(1 2).
	self
		assert: ([ :first :second :third | Array with: first with: second with: third ] glamourValueWithArgs: #(1 2))
		equals: #(1 2 nil).	"Symbol"
	self assert: (#yourself glamourValueWithArgs: #(1 2)) equals: 1.
	self assert: (#yourself glamourValueWithArgs: #()) equals: nil
]
