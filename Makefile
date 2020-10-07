##############################################################
#   Finite Element Solver FERITAS 
#   Makefile for building application
#
#   Toshiro Matsumoto
#   Department of Mechanical Science and Engineering
#   Nagoya Univerisity
#   Furo-cho, Chikusa-ku, Nagoya, 464-8603, Japan
#   E-mail: t.matsumoto@nuem.nagoya-u.ac.jp
#   Copyright 2009 Toshiro Matsumoto. All rights reserved.
#   REVISION DATE: 13-Dec-2009
##############################################################


# Compiler
FC  = gfortran

# Linker
LD  = gfortran

# When specifying libraries, library path is defined.
#LIB =  -L../lib -lfile

.SUFFIXES : .f90

TARGET = app/feritas

SRC  = PrecTypes.f90 \
		GlobalData.f90 \
		Gauss.f90 \
		EPS_sharedModule.f90 \
		BandWidth.f90 \
		CheckTime.f90 \
		EPS_deformation.f90 \
		EPS_mesh.f90 \
		EPS_mises.f90 \
		EPS_temp.f90 \
		EPSoutput.f90 \
		EndingMessage.f90 \
		F_Elast2Linear.f90 \
		F_Poten2Linear.f90 \
		Feritas.f90 \
		Flux_2linear.f90 \
		Force_2linear.f90 \
		Input.f90 \
		K_Elast2RectLinear.f90 \
		K_Elast2TrigLinear.f90 \
		K_Poten2RectLinear.f90 \
		K_Poten2TrigLinear.f90 \
		LUfact.f90 \
		Mat_Elast2RectLinear.f90 \
		Mat_Elast2TrigLinear.f90 \
		Mat_Poten2RectLinear.f90 \
		Mat_Poten2TrigLinear.f90 \
		Matrix.f90 \
		Output.f90 \
		Preprocess.f90 \
		SS_2RectLinear.f90 \
		SS_2TrigLinear.f90 \
		Solve.f90 \
		StartingMessage.f90 \
		StressStrain.f90


OBJ  = PrecTypes.o \
		GlobalData.o \
		Gauss.o \
		EPS_sharedModule.o \
		BandWidth.o \
		CheckTime.o \
		EPS_deformation.o \
		EPS_mesh.o \
		EPS_mises.o \
		EPS_temp.o \
		EPSoutput.o \
		EndingMessage.o \
		F_Elast2Linear.o \
		F_Poten2Linear.o \
		Feritas.o \
		Flux_2linear.o \
		Force_2linear.o \
		Input.o \
		K_Elast2RectLinear.o \
		K_Elast2TrigLinear.o \
		K_Poten2RectLinear.o \
		K_Poten2TrigLinear.o \
		LUfact.o \
		Mat_Elast2RectLinear.o \
		Mat_Elast2TrigLinear.o \
		Mat_Poten2RectLinear.o \
		Mat_Poten2TrigLinear.o \
		Matrix.o \
		Output.o \
		Preprocess.o \
		SS_2RectLinear.o \
		SS_2TrigLinear.o \
		Solve.o \
		StartingMessage.o \
		StressStrain.o


all: $(TARGET) copy

$(TARGET): $(OBJ)
	$(LD) $(LOPT) -o $@ $(OBJ) $(LIB)
	
%.o: %.f90
	$(FC) $(LOPT) $(INC) -o $@ -c $< 
	
#.f90.o:
#	$(FC) -c $<

clear:
	rm -f *.o *~ *.mod core
	rm -f /app/feritas

cleanall:
	rm -f *.o *~ *.mod core
	rm -f app/feritas
	rm -f app/elast/feritas
	rm -f app/elast/*.eps
	rm -f app/elast/mesh*
	rm -f app/elast/contour*
	rm -f app/elast/deform*
	rm -f app/elast/output*
	rm -f app/thermoelast/feritas
	rm -f app/thermoelast/*.eps
	rm -f app/thermoelast/mesh*
	rm -f app/thermoelast/contour*
	rm -f app/thermoelast/deform*
	rm -f app/thermoelast/output*
	rm -f app/heat/feritas
	rm -f app/heat/*.eps
	rm -f app/heat/mesh*
	rm -f app/heat/contour*
	rm -f app/heat/output*

clean:
	rm -f *.o *~ *.mod core
	rm -f app/elast/*.eps
	rm -f app/elast/mesh*
	rm -f app/elast/contour*
	rm -f app/elast/deform*
	rm -f app/elast/output*
	rm -f app/thermoelast/*.eps
	rm -f app/thermoelast/mesh*
	rm -f app/thermoelast/contour*
	rm -f app/thermoelast/deform*
	rm -f app/thermoelast/output*
	rm -f app/heat/*.eps
	rm -f app/heat/mesh*
	rm -f app/heat/contour*
	rm -f app/heat/output*

copy:
	cp app/feritas app/elast/
	cp app/feritas app/heat/
	cp app/feritas app/thermoelast/
   