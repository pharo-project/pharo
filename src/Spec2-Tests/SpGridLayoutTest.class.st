Class {
	#name : #SpGridLayoutTest,
	#superclass : #TestCase,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #tests }
SpGridLayoutTest >> testInitialStatus [
	| layout |

	layout := SpGridLayout new.
	
	self assert: layout isColumnHomogeneous.	
	self assert: layout isRowHomogeneous.
	self assert: layout columnSpacing equals: 10.
	self assert: layout rowSpacing equals: 5.
	self assert: layout borderWidth equals: 20
]
