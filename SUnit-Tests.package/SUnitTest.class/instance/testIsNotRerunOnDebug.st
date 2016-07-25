testing
testIsNotRerunOnDebug

	| case |

	case := self class selector: #testRanOnlyOnce.
	case run.
	case debug