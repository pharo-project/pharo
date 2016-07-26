scheduling
forkAt: priority named: name

	"Create and schedule a Process running the code in the receiver at the

	given priority and having the given name. Answer the newly created 

	process."



	| forkedProcess |

	forkedProcess := self newProcess.

	forkedProcess priority: priority.

	forkedProcess name: name.

	^ forkedProcess resume