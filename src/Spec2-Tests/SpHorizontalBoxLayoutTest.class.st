Class {
	#name : #SpHorizontalBoxLayoutTest,
	#superclass : #SpBoxLayoutTest,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #running }
SpHorizontalBoxLayoutTest >> initializeTestedInstance [

	layout := SpBoxLayout newHorizontal.
	presenter layout: layout
]

{ #category : #tests }
SpHorizontalBoxLayoutTest >> testPresenterExtentFollowsChildrenExtent [
	| label button |

	layout 
		add: ((label := presenter newLabel) label: 'Label1');
		add: ((button := presenter newButton) label: 'Button1').
		
	self openInstance.
	
	self assert: (self widthOf: presenter) >= ((self widthOf: label) + (self widthOf: button)).
	self assert: (self heightOf: presenter) >= ((self heightOf: label) max: (self heightOf: button))
]
