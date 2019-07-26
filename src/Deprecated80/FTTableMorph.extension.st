Extension { #name : #FTTableMorph }

{ #category : #'*Deprecated80' }
FTTableMorph >> headerColor: aColor [
	self
		deprecated:
			'This method will be removed in the next Pharo version. The header color is not the responsibility of the FastTable. If you want to change the color of the table''s header you should do it in the datasource. In the datasource there is a method to return a header''s cell, you should change the color there.'
		on: '29-04-2019'
		in: #Pharo8.
	"When this method will be removed, remove also #headerColor and related methods."
	headerColor := aColor
]
