I'm the base class for "stream compressor". For example, my subclass GZipWriteStream can compress a stream contents using gzip algorithm.

Try:

gzData := String streamContents: [:aStream|
         (GZipWriteStream on: aStream)
                nextPutAll: 'Some data to be gzipped';
                close. ].
Transcript 
        show: gzData; 
        cr;
        show: (GZipReadStream on: gzData) upToEnd;
        cr.

See InflateStream