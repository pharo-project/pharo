Extension { #name : #StHeaderBar }

{ #category : #'*NewTools-Debugger-Extensions' }
StHeaderBar >> applyStyleToLabel: aStyleName [
	titleLabel addStyle: aStyleName
]

{ #category : #'*NewTools-Debugger-Extensions' }
StHeaderBar >> removeStyleFromLabel: aStyleName [
	titleLabel removeStyle: aStyleName
]
