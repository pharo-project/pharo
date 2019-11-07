Extension { #name : #String }

{ #category : #'*Graphics-Canvas' }
String >> displayAt: aPoint [ 
	"Display the receiver as a DisplayText at aPoint on the display screen."

	self displayOn: Display at: aPoint
]

{ #category : #'*Graphics-Canvas' }
String >> displayOn: aDisplayMedium [
	"Display the receiver on the given DisplayMedium."

	self displayOn: aDisplayMedium at: 0 @ 0
]

{ #category : #'*Graphics-Canvas' }
String >> displayOn: aDisplayMedium at: aPoint [ 
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, using black-colored text."

	self displayOn: aDisplayMedium at: aPoint textColor: Color black
]

{ #category : #'*Graphics-Canvas' }
String >> displayOn: aDisplayMedium at: aPoint textColor: aColor [
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, rendering the text in the designated color"
	aDisplayMedium getCanvas drawString: self from: 1 to: self size at: aPoint font: nil color: aColor

]
