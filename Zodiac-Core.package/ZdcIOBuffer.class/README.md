I am ZdcIOBuffer.

I manage a fixed SequenceableCollection buffer for simultaneous input and output.
I maintain a readPointer and a writePointer.

When data is written to me, it is stored at the end, past my writePointer.
When data is read from me, it is retrieved from the front, past my readPointer.

Invariant: readPointer <= writePointer

My valid contents for reading is defined from contentsStart to contentsEnd, from readPointer + 1 to writePointer.
Data can be added to the free space defined from freeSpaceStart to freeSpaceEnd, from writePointer + 1 to the buffer's' size.

There can be a gap at my front. Compacting moves data if necessary to make (more) room at the end.