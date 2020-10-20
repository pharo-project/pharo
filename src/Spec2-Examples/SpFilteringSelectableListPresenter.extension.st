Extension { #name : #SpFilteringSelectableListPresenter }

{ #category : #'*Spec2-Examples' }
SpFilteringSelectableListPresenter class >> example [
	"<sampleInstance>"
	| example |

	example := self new.
	example items: RBBrowserEnvironment default classes asArray.
	example openWithSpec.
	example withWindowDo: [ :window | 
		window title: self name asString , ' example' ].

	^ example
]

{ #category : #'*Spec2-Examples' }
SpFilteringSelectableListPresenter class >> exampleWithInitializedFilter [
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

{ #category : #'*Spec2-Examples' }
SpFilteringSelectableListPresenter class >> exampleWithInitializedFilterAndSeletion [
	| example |

	example := self new.
	example matchSubstring.
	example items: RBBrowserEnvironment default classes asArray.
	example displayIcon: [ :aClass | aClass systemIcon ].
	example applyFilter: 'ZZ'.
	example selectedItems: { FuzzyMatcher. FuzzyMatcherExample }.
	example openWithSpec title:
		self name asString , ' example with filter'.

	^ example
]
