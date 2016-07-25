private
maybeDismiss: evt with: dismissHandle 
	"Ask hand to dismiss my target if mouse comes up
	in it." 

	| confirmed |
	evt hand obtainHalo: self.
	(dismissHandle containsPoint: evt cursorPoint)
		ifTrue: [
			target resistsRemoval ifTrue: [
				confirmed := self confirm: 'Really throw this away?' translated.
				confirmed ifFalse: [^ self]].
			evt hand removeHalo.
			self delete.
			target dismissViaHalo]
		ifFalse: [
			self delete.
			target addHalo: evt]