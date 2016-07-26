*metacello-mc
loadUsing: aLoader gofer: ignored

	| required |
	required := self resolveToLoadableSpec.
	required loader: aLoader.
	^required load