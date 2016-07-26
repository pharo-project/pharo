I am ZnCommonLogFormat, I can output ZnServerTransactionEvent objects using Apache Common Log Format (CLF).

https://en.wikipedia.org/wiki/Common_Log_Format
https://httpd.apache.org/docs/trunk/logs.html#common

| formatter |
formatter := ZnCommonLogFormat new.
ZnLogEvent announcer 
  when: ZnServerTransactionEvent 
  do: [ :event | 
    formatter format: event on: Transcript.
    Transcript cr; endEntry ].
