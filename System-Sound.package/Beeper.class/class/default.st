customize
default
	"When the default is not defined it is
	initialized using #newDefault."

	^ default ifNil: [default := self newDefault ] 