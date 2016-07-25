testing
testRan

	| case |

	case := self class selector: #setRun.
	case resources do: [:each | each availableFor: case].
	[case setUp.
	case performTest] ensure: [
		self assert: case hasSetup.
		self assert: case hasRun.
		case tearDown.
		case cleanUpInstanceVariables].
	self assert: case hasSetup isNil.
	self assert: case hasRun isNil.