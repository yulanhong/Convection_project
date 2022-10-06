
        subroutine Julday2month(year,Julday,month,day)

        implicit none
        integer :: year,month,day,Julday

        If (mod(year,4) ==0 ) then
        
        select case (Julday)
                case (1:31)
                month=1
                day=Julday        
                case (32:60)
                month=2   
                day=Julday-31     
                case (61:91)
                month=3
                day=Julday-60     
                case (92:121)
                month=4        
                day=Julday-91
                case (122:152)
                month=5      
                day=Julday-121  
                case (153:182)
                month=6       
                day=Julday-152 
                case (183:213)
                month=7       
                day=Julday-182 
                case (214:244)
                month=8       
                day=Julday-213 
                case (245:274)
                month=9       
                day=Julday-244 
                case (275:305)
                month=10      
                day=Julday-274  
                case (306:335)
                month=11      
                day=Julday-305  
                case (336:366)
                month=12      
                day=Julday-335  
        end select

        else  

        select case (Julday)
                case (1:31)
                month=1
                day=Julday
                case (32:59)
                month=2   
                day=Julday-31     
                case (60:90)
                month=3
                day=Julday-59
                case (91:120)
                month=4      
                day=Julday-90  
                case (121:151)
                month=5      
                day=Julday-120  
                case (152:181)
                month=6       
                day=Julday-151 
                case (182:212)
                month=7       
                day=Julday-181 
                case (213:243)
                month=8       
                day=Julday-212 
                case (244:273)
                month=9       
                day=Julday-243 
                case (274:304)
                month=10      
                day=Julday-273  
                case (305:334)
                month=11      
                day=Julday-304  
                case (335:365)
                month=12      
                day=Julday-334  
        end select

        EndIf

                
        end
