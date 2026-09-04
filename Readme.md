Bytestring --decodeHeader--> (Object id, opcode, remaining bytestring) --???--> (Corresponding interface and event, bytestring) --decodeEvent--> message --???--> User supplied callback $ values 

Bytestring --decodeHeader--> (Object id, opcode, remaining bytestring) --???--> (Corresponding interface and event, bytestring) --decodeEvent--> message including new id --register in map of object id and ???--> registered object --???--> User supplied callback $ values 

User normal request --encode arguments--> message --encode message--> message --send off to socket--> IO()

User new_id request --allocate new id--> new id and args --register new id and ???--> registering done --encode message--> message --send off to socket--> IO(new id)

InterfaceType = InterfaceType 
  {
    decoder:: (Opcode, Bytestring) -> ([Value] , Bytestring)
  }

