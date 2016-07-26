private
ensureColorArrayExists
	"Return my color palette."

	colors ifNil: [
		self depth > 8 ifTrue: [^ self error: 'ColorForms only support depths up to 8 bits'].
		self colors: (Color indexedColors copyFrom: 1 to: (1 bitShift: self depth))].
