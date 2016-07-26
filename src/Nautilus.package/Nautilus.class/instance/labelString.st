browser compatibility
labelString
	^ self ui ifNil: [ 'Nautilus - System Browser' ] ifNotNil: [ :theui | theui title ]