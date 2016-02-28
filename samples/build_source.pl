#!C:\perl\bin\perl.exe
use Date::Calc qw(:all);
$gms_to_mgs=1000.;
$mw_to_kcal_per_sec=859.845*1.0e06/(3600*1000);
#
# Modication to read in argument from command line (WUR_WF_MvV_2011/05/19) 
chomp($project="@ARGV[0]");
#
#  Open project thermal file
#
#open THERMAL,$project.'_thermal.inp';
$dmmy=10;
$chloride_inp = $project.'_cldump';
open CHLORIDE,"$chloride_inp";
$cl_background_file=$project.'_Cl_background';
print "$cl_background_file\n";
open CL_BCKGRND,$cl_background_file or die "Cannot open background file";
$temp_inp=$project.'_heatdump';
open THERMAL,"$temp_inp";
chomp($zz=<THERMAL>);
($dmmy,$ncol)=split/\s+/,$zz;
print "Columns $chloride_inp $temp_inp $ncol\n";
chomp($zz=<THERMAL>); <CHLORIDE>; $qq=<CL_BCKGRND>; {print "QQ $qq\n";}
($dmmy,$nrow)=split/\s+/,$zz;
print " Blug Rows $zz $nrow\n";
<THERMAL>;<THERMAL>;<THERMAL>;<THERMAL>;
<CHLORIDE>;<CHLORIDE>;<CHLORIDE>;
<CL_BCKGRND>; <CL_BCKGRND>; <CL_BCKGRND>;
for $nr (1..$nrow) {
   $nrr=$nrow-$nr+1;
   chomp($zz_T=<THERMAL>);
   (@cell_col_T)=split/\s+/,$zz_T;
   chomp($zz_Cl=<CHLORIDE>);
   (@cell_col_Cl)=split/\s+/,$zz_Cl;
   chomp($zz_Cl_back=<CL_BCKGRND>);
#   print "$zz_Cl_back\n";
   (@cell_col_Cl_back)=split/\s+/,$zz_Cl_back;
   for $nc (1..$ncol) {
      if($cell_col_T[$nc-1]<=0.0) {$cell_col_T[$nc-1]=0.0;}
      $thermal[$nrr][$nc]=$cell_col_T[$nc-1];
      if($cell_col_Cl[$nc-1]<=0.0) {$cell_col_Cl[$nc-1]=0.0;}
      $chloride[$nrr][$nc]=$cell_col_Cl[$nc-1];
      if($cell_col_Cl_back[$nc-1]<=0.0) {$cell_col_Cl_back[$nc-1]=0.0;}
      $chloride_back[$nrr][$nc]=$cell_col_Cl_back[$nc-1];
 #     print "$nr $chloride_back[$nrr][$nc] $cell_col_Cl_back[$nc-1]\n";
   }
}
close CHLORIDE;
close CL_BCKGRND;
close THERMAL;
# Open project Rout.Cell.init file
# Modication to read in argument from command line (WUR_WF_MvV_2011/05/19)
#
$infile= $project.'_Rout.Cells.init';
open INFILE, "$infile";
#
#
# Define project Point.Source output file
# Modication to read in argument from command line (WUR_WF_MvV_2011/05/19)
#
$T_out=$project.'_T_PointSource';
open T_OUT, ">$T_out";
$Cl_out=$project.'_Cl_PointSource';
open CL_OUT, ">$Cl_out";
$Cl_head=$project.'_Cl_headwaters';
open CL_HEAD,">$Cl_head";
$Cl_back_out=$project.'_Cl_background';
open CL_BACK,">$Cl_back_out";
#
#  Read header
#
chomp($zz=<INFILE>);
($nhead,$nseg)=split/\s+/,$zz;
print "$nhead $nseg\n";
$head='TRUE';
for $ns (1..$nseg) {
   chomp($zz=<INFILE>);
   ($ncntrl,$nss,$lat_long,$icol,$irow)=split/\s+/,$zz;
   $icol_adj=$icol+1;
   if ($head eq 'TRUE') {
      printf CL_HEAD "%5d %10.1f\n",$ns, $chloride_back[$irow][$icol];
      $head='FALSE';
      print "$ns $head\n";
   }    
   if ($ncntrl== 0) {
     $head = 'TRUE';
   }  
#
   $heat=$mw_to_kcal_per_sec*$thermal[$irow][$icol];
   $chloride=$chloride[$irow][$icol];
   $clback=$chloride_back[$irow][$icol];
   printf T_OUT "%5d %10.1f %10.1f\n",$ns,$heat,$thermal[$irow][$icol];
   printf CL_OUT "%5d %15.1f %15.1f\n",$ns,$chloride,$chloride[$irow][$icol];
   printf CL_BACK "%5d %15.1f %15.1f\n",$ns,$clback,$chloride_back[$irow][$icol];
   if ($ncntrl==1) {<INFILE>;}
}   
close INFILE;
close T_OUT;
close Cl_OUT;

