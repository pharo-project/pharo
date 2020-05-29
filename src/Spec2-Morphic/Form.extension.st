Extension { #name : #Form }

{ #category : #'*Spec2-Morphic' }
Form >> asPresenter [

	^ SpImagePresenter new 
		image: self;
		yourself
]
