This implements the Collecting Parameter pattern for running a bunch of tests.  It holds tests that have run, sorted into the result categories of passed, failures and errors.

TestResult is an interesting object to subclass or substitute. #runCase: is the external protocol you need to reproduce. TestResult subclasses can  handle multi-threaded tests (see SUnitXProcPatterns) and might record coverage information or send emails when the run completes.
