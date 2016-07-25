finalization
finalize
	"Finalize the resource associated with the receiver. This message should only be sent during the finalization process. There is NO garantuee that the resource associated with the receiver hasn't been free'd before so take care that you don't run into trouble - this all may happen with interrupt priority."