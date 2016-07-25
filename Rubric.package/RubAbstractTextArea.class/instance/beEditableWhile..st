public accessing
beEditableWhile: aBlock
	| wasEditable |
	wasEditable := self readOnly.
	self readOnly: false.
	aBlock ensure: [ self readOnly:  wasEditable]
