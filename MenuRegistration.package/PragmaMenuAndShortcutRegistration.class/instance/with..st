menu protocol
with: aBlock
	| old itemOrGroup |
	old := self currentRoot.
	itemOrGroup := self currentItem item ifNil: [ self currentItem group ].
	self currentRoot: itemOrGroup.
	[ aBlock value ]
		ensure: [ self currentRoot: old ]