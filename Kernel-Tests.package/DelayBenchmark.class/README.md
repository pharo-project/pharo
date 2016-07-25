The delay scheduling loop is THE highest priority code which is run in Pharo, in other words it is time-critical. The speed of this code is critical for accurate responses, it is critical for network services, it affects every last part of the system.

DelayBenchmark provides a means for evaluating modifications.  This is fairly basic, providing only Transcript output of results.

Over a number of trials the amount of concurrent delays is increased.  The durations are randomly pre-generated into an array to avoid that overhead in the trials.  A fixed seed is used to try and improve comparisons between runs. Uncomment the additional seeds to provide better averaging of results.