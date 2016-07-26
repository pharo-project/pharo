Parameterized implementation of the cyclic redundancy check (CRC) algorithm.

INTRODUCTION
=================
This implementation is based on the (awesome) paper "A Painless Guide to CRC Error Detection Algorithms" by Ross Williams. You should find a copy of the paper here: http://www.ross.net/crc/. In this paper Ross describes a parameterized implementation that enables the different variations of the CRC algorithm to be used in a consistent way, simply by adjusting the parameters. If you don't have a clue about CRC (like me) then I strongly suggest reading the paper. It will also help you to understand how to make the best use of this implementation.

The "CRC RevEng" project on sourceforge implements Williams's "RockSoft" parameterized CRC program (as does this class) and comes with a handy list of parameters for various CRC algorithm: http://reveng.sourceforge.net/crc-catalogue/.

For ease of use and better performance, the two defacto standard variations "CRC16" and "CRC32" have been predefined. The lookup tables for these implementations are included on the class side. For all other variations the lookup table will be generated at runtime before the first run.

If you want to define your own algorithm you can do so by using the methods in the "accessing-parameters" protocol. Note that there are no default values. Here's a short overview:
	#width: 			defines the width of the register (usually 16 or 32)
	#polynome: 		defines the polynome to use for polynome division / lookup table creation
	#registerFill: 		defines the start content of the working register (usually all ones or all zeros)
	#reflectInput: 		if true every byte will be reflected before processing (e.g. 100101 -> 101001)
	#reflectOutpu: 		if true the entire register will be reflected before the final XOR stage
	#finallyXorWith: 	defines the final XOR for the entire register
	#lookupTable: 		the only OPTIONAL parameter. The lookup table will be generated at runtime if none has been supplied
	#message: 			the message to calculate the CRC on 
		

EXAMPLES
=================
The simplest possible snippet uses the class side methods for "CRC16" and "CRC32":
	CRC crc16FromCollection: 'some message'. --> 55709
	CRC crc32FromCollection: 'some message'. --> 191312361

Let's assume, you wanted to use "CRC16 reversed" (neither input nor output reflected). Then you would have to change the parameters like so (the reversed form uses a different polynome and a different start register content):
	crc := CRC new
		beCrc16;
		polynome: 16r1021;
		registerFill: 16rFFFF;
		reflectInput: false;
		reflectOutput: false;
		message: 'some message';
		yourself.
	crc run. --> 46785
	
Using a single instance as in the code above will of course be faster than using the class side methods when performing multiple runs. But if you are really concerned about performance (see PERFORMANCE) you should use the "raw" methods (no checks! If you forget to set parameters there will be errors....):
	crc := CRC new
		beCrc16;
		message: 'some message';
		yourself.
	crc runRefInRefOut. --> 55709
	
	crc := CRC new
		beCrc16;
		polynome: 16r1021;
		registerFill: 16rFFFF;
		message: 'some message';
		yourself.
	crc runNonRefInNonRefOut. --> 46785
	

PERFORMANCE
=================
The performance of this implementation (tested for crc16) is equal to the performance of String>>crc16 if executed "raw" (see EXAMPLES). For the users sake however, the implementation does a few extra checks to improve ease of use. The cost is a loss of performance of about factor 1.15 (single instance) and 1.42 (one instance per run) (note that although I took an average of 10, the results will vary quite a bit each time you run the code):
	crc := CRC new 
		beCrc16; 
		message: 'this is a test message'; 
		yourself.
	
	"String>>crc16"	
	times := OrderedCollection new.
	10 timesRepeat: [ times add: [ 1000000 timesRepeat: [ 'this is a test message' crc16 ] ] timeToRun ].
	times average floor. --> 530
	
	"raw"
	times := OrderedCollection new.
	10 timesRepeat: [ times add: [ 1000000 timesRepeat: [ crc runRefInRefOut ] ] timeToRun ].
	times average floor. --> 535
	
	"user friendly, one instance"	
	times := OrderedCollection new.
	10 timesRepeat: [ times add: [ 1000000 timesRepeat: [ crc run ] ] timeToRun ].
	times average floor. --> 616
	
	"user friendly, one instance per run"
	times := OrderedCollection new.
	10 timesRepeat: [ times add: [ 1000000 timesRepeat: [ CRC crc16FromCollection: 'this is a test message' ] ] timeToRun ].
	times average floor. --> 759