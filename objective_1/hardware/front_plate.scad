
difference(){
import("/home/nicoluarte/repos_sync/PHD_data/objective_1/hardware/front_plate.stl");


translate([-10,35,4])
rotate([90,-90,-90])
linear_extrude(100, center = true)
text( "V2", size= 7, font="FreeMono:style=Bold");
}

translate([-13.7,0,26])
cube([1,22.5,4], center = true);

translate([-8.2,10.75,29])
cube([12,1,10], center = true);

translate([-8.2,-10.75,29])
cube([12,1,10], center = true);

translate([-9.7,0,36])
cube([9,22.5,4], center = true);