testing - enumeration
testAllInstructions
		| ir |

		ir := IRBuilderTest new testPushTempTemp.
		self assert: ir allInstructions last isReturn.