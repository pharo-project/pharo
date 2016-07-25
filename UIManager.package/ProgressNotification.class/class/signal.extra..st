exceptioninstantiator
signal: signalerText extra: extraParam
	"TFEI - Signal the occurrence of an exceptional condition with a specified textual description."

	| ex |
	ex := self new.
	ex extraParam: extraParam.
	^ex signal: signalerText