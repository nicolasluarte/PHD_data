

$fn = 100;

difference(){
    import("/home/nicoluarte/repos_sync/PHD_data/objective_1/hardware/disk.stl");
for(i = [1:1:8]){
translate([sin(360*i/8)*16, cos(360*i/8)*16, 0 ])
cylinder(100, d=5);
}
}