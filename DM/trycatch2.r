tryCatch(
	{
           x = 2
           z = sqrt(x)
         },
         
         # �J�� warning �ɪ��ۭq�B�z���
         warning = function(msg) {
              message(paste("[Warning]",msg,"\n"))
              return(NULL)
         },
         
         # �J�� error �ɪ��ۭq�B�z���
         error = function(msg) {
              message(paste("[Error]",msg,"\n"))
              return(NA)
      }
) 
ifelse(exists("z"),z,"z does not exist!")
a= x
a
