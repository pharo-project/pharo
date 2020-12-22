Extension { #name : #SpProgressBarPresenter }

{ #category : #'*Spec2-Examples' }
SpProgressBarPresenter class >> example [
	| progress |

	progress := 0.
	^ self new 
		progress: [ progress:= progress + 0.1 ] every: 0.5 seconds;
		openWithSpec
]

{ #category : #'*Spec2-Examples' }
SpProgressBarPresenter class >> exampleFixedAt [

	^ self new 
		fixedAt: 0.3;
		openWithSpec
]

{ #category : #'*Spec2-Examples' }
SpProgressBarPresenter class >> exampleFixedPercentage [

	^ self new 
		fixedPercentage: 30;
		openWithSpec
]
