bytecode generation
genPushNClosureTemps: numTemps
	"Not supported anymore since we're migrating to FullBlockClosure"
	self notify: 'This instruction should not be used any more'.
	"backward compatibility..."
	numTemps timesRepeat: [ self genPushSpecialLiteral: nil ]