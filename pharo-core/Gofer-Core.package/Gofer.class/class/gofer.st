private
gofer
	"Create a Gofer instance of Gofer."

	^ self new
		renggli: 'gofer';
		package: 'Gofer-Core';
		package: 'Gofer-Tests';
		yourself