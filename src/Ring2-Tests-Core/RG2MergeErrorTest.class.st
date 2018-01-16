Class {
	#name : #RG2MergeErrorTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2MergeErrorTest >> testErrorInformation [

	| error def1 def2 |

	error := nil.
	def1 := RG2BehaviorDefinition named: #SomeDefinition1.
	def2 := RG2BehaviorDefinition named: #SomeDefinition2.

	[def1 merge: def2] on: RG2MergeError do: [ :exception | error := exception ].

	self assert: error notNil.
	self assert: error property equals: #name.
	self assert: error target equals: def1.
	self assert: error source equals: def2.
	
]
