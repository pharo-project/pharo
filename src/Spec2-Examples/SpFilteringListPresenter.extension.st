Extension { #name : #SpFilteringListPresenter }

{ #category : #'*Spec2-Examples' }
SpFilteringListPresenter class >> example [
	<sampleInstance>
	| example |

	example := self new.
	example items: RBBrowserEnvironment default classes asArray.
	example openWithSpec.
	example withWindowDo: [ :window | 
		window title: self name asString , ' example' ].

	^ example
]

{ #category : #'*Spec2-Examples' }
SpFilteringListPresenter class >> exampleWithFilterOnTop [
	<sampleInstance>
	| example |

	example := self new.
	example items: RBBrowserEnvironment default classes asArray.
	example openWithSpec: #topLayout.
	example withWindowDo: [ :window | 
		window title: self name asString , ' example' ].

	^ example
]

{ #category : #'*Spec2-Examples' }
SpFilteringListPresenter class >> exampleWithInitializedFilter [

	<sampleInstance>
	| example |
	example := self new.
	example matchSubstring.
	example items: RBBrowserEnvironment default classes asArray.
	example displayIcon: [ :aClass | aClass systemIcon ].
	example applyFilter: 'ZZ'.
	example openWithSpec title:
		self name asString , ' example with filter'.

	^ example
]
