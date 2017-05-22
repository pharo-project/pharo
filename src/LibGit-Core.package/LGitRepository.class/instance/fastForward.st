operations
fastForward
	<todo: 'check for conflicts'>
	"currently forces fastForward in every case"
	self head resolve setTarget: (self lookup: 'FETCH_HEAD')