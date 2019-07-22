Extension { #name : #CmdMenu }

{ #category : #'*Commander-Spec2-Compatibility' }
CmdMenu >> asSpMenuPresenter [
	| menu |
	menu := SpMenuPresenter new.
	rootGroup contents do: [ :each | each addToMenuPresenter: menu  ].
	^menu
]
