Extension { #name : #RubUnderlinedSegmentMorph }

{ #category : #'*NewTools-SpTextPresenterDecorators' }
RubUnderlinedSegmentMorph class >> on: aTextPresenterDecorator [
	^ (self
		   from: aTextPresenterDecorator start
		   to: aTextPresenterDecorator stop)
		  color: aTextPresenterDecorator highlightColor;
		  underlineColor: aTextPresenterDecorator underlineColor;
		  icon: aTextPresenterDecorator icon;
		  iconBlock: aTextPresenterDecorator iconBlock;
		  label: aTextPresenterDecorator title;
		  yourself
]
