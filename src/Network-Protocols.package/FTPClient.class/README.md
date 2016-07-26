A minimal FTP client program.  Could store all state in inst vars, and use an instance to represent the full state of a connection in progress.  But simpler to do all that in one method and have it be a complete transaction.

Always operates in passive mode (PASV).  All connections are initiated from client in order to get through firewalls.

See ServerDirectory openFTP, ServerDirectory getFileNamed:, ServerDirectory putFile:named: for examples of use.

See TCP/IP, second edition, by Dr. Sidnie Feit, McGraw-Hill, 1997, Chapter 14, p311.