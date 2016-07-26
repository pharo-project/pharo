coverage
selectorsToBeIgnored

	 | deprecated private special primitives timing benchmarks |

	deprecated := #().
	private := #( #print24:on: #print24:showSeconds:on: ).
	special := #( #< #= #new #printOn: #storeOn: ).
	primitives := #( #primMillisecondClock #primSecondsClock #primUTCSecondsClock #primUTCMicrosecondsClock #primUTCMillisecondsClock).
	timing := #( #millisecondClockValue #milliseconds:since: #millisecondsSince: ).
	benchmarks := #( #benchmarkMillisecondClock #benchmarkPrimitiveResponseDelay ). 

	^ super selectorsToBeIgnored, deprecated, private, special, primitives, timing, benchmarks.