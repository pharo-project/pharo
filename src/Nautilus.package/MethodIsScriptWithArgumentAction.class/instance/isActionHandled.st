testing
isActionHandled
	"Return true if the method is tagged with <script: '...'> pragma for 	      evaluating code"

	^ method pragmas anySatisfy: [ :each | each selector = #script: ]