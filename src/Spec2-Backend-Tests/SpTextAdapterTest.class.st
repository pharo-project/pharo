Class {
	#name : #SpTextAdapterTest,
	#superclass : #SpAbstractTextAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpTextAdapterTest >> classToTest [

	^ SpTextPresenter
]

{ #category : #accessing }
SpTextAdapterTest >> testKeyBindings [
	| handled |

	handled := false.
	presenter 
		bindKeyCombination: $x meta 
		toAction: [ handled := true ].

	self adapter 
		keyDown: $x 
		shift: false 
		meta: true
		control: false
		option: false. 

	self assert: handled
]

{ #category : #accessing }
SpTextAdapterTest >> testRightClickShowsMenu [
	| menu |

	presenter contextMenu: (menu := SpMockMenuPresenter new).

	self deny: menu shown.
	self emulateRightClick.
	self assert: menu shown
]
