evaluating
cull: arguments
	"in case of my wrapped link expects arguments"

	^ condition
		and: [ linkCondition perform: linkCondition valueSelector withArguments: {arguments} ]