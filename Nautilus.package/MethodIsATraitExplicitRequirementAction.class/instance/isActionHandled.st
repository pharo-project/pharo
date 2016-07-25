order
isActionHandled
	^method isRequired and: [ method methodClass ~= method origin ]