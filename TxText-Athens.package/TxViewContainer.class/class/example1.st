examples
example1

	| view |

	view := TxViewContainer new.

	view addDecorator: (TxViewDecoratorMorph new placeTop color: Color red;yourself).
	view addDecorator: (TxViewDecoratorMorph new placeRight color: Color yellow;yourself).
	view addDecorator: (TxViewDecoratorMorph new placeLeft  color: Color blue;yourself).
	view addDecorator: (TxViewDecoratorMorph new placeBottom color: Color green;yourself).

	view  openInWindow


