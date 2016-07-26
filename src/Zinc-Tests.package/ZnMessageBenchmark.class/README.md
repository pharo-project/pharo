I am ZnMessageBenchmark helps to test the benchmarking and profiling of ZnMessage writing and reading.

Instance Variables
	buffer:					<ByteArray>
	message:				<ZnObject>
	representation:		<ByteArray>

ZnMessageBenchmark new
	simpleRequest;
	write: 10000.

ZnMessageBenchmark new
	simpleRequest;
	writeRepresentation;
	read: 10000.

ZnMessageBenchmark new
	simpleResponse;
	write: 10000.

ZnMessageBenchmark new
	simpleResponse;
	writeRepresentation;
	read: 10000.
