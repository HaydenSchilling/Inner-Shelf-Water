# velocity rotation (to align with coastline):  Hayden Schilling adapted from Amandine's Matlab code
# # Cb = 356 degree, EH = 13 degree, NS = 15 degree, DH = 19 degree

## Matlab code from amandine 
##  rot_deg_angle=-22 # to change
#  
#
## UCUR_shore=cosd(rot_deg_angle).*UCUR+sind(rot_deg_angle).*VCUR; % across-shelf 
## VCUR_shore=-sind(rot_deg_angle).*UCUR+cosd(rot_deg_angle).*VCUR; % along-shelf

mydata$U_shore = 0
mydata$V_shore = 0
for (i in 1:nrow(mydata)){
  if (mydata$OPC_site[i] == "CB") {
    rot_deg_angle= -356
    mydata$U_shore[i] = cos(rot_deg_angle*pi/180)*mydata$U[i] + sin(rot_deg_angle*pi/180)*mydata$V[i]
    mydata$V_shore[i] = sin(rot_deg_angle*pi/180)*mydata$U[i] + cos(rot_deg_angle*pi/180)*mydata$V[i]
  }
  if (mydata$OPC_site[i] == "DH") {
    rot_deg_angle= -19
    mydata$U_shore[i] = cos(rot_deg_angle*pi/180)*mydata$U[i] + sin(rot_deg_angle*pi/180)*mydata$V[i]
    mydata$V_shore[i] = sin(rot_deg_angle*pi/180)*mydata$U[i] + cos(rot_deg_angle*pi/180)*mydata$V[i]
  }
  if (mydata$OPC_site[i] == "EH") {
    rot_deg_angle= -13
    mydata$U_shore[i] = cos(rot_deg_angle*pi/180)*mydata$U[i] + sin(rot_deg_angle*pi/180)*mydata$V[i]
    mydata$V_shore[i] = sin(rot_deg_angle*pi/180)*mydata$U[i] + cos(rot_deg_angle*pi/180)*mydata$V[i]
  }
  if (mydata$OPC_site[i] == "NS") {
    rot_deg_angle= -15
    mydata$U_shore[i] = cos(rot_deg_angle*pi/180)*mydata$U[i] + sin(rot_deg_angle*pi/180)*mydata$V[i]
    mydata$V_shore[i] = sin(rot_deg_angle*pi/180)*mydata$U[i] + cos(rot_deg_angle*pi/180)*mydata$V[i]
  }
}
