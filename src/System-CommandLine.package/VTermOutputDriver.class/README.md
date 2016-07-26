I convert high-level terminal commands like moving the cursor or setting the color to characters sequences understood by the terminal.

Furthermore I do some bookkeeping to reduce the number of characters sent to the terminal.

Example of usage:

	out := VTermOutputDriver stdout.
	out << 'normal text'.
	out lf.
	'red text' do: [ :c | out color256: Color red. out << c ].
	out lf.
	'bold text' do: [ :c | out bold. out << c ].
	out clear.
	out lf.