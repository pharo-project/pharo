Class {
	#name : #SpVerticalBoxLayoutTest,
	#superclass : #SpBoxLayoutTest,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #running }
SpVerticalBoxLayoutTest >> initializeTestedInstance [

	layout := SpBoxLayout newVertical.
	presenter layout: layout
]

{ #category : #tests }
SpVerticalBoxLayoutTest >> testPresenterExtentFollowsChildrenExtent [
	| label button |

	layout 
		add: ((label := presenter newLabel) label: 'Label1');
		add: ((button := presenter newButton) label: 'Button1').
		
	self openInstance.
	
	self assert: (self widthOf: presenter) >= ((self widthOf: label) max: (self widthOf: button)).
	self assert: (self heightOf: presenter) >= ((self heightOf: label) + (self heightOf: button))
]
