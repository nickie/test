<?php
 
// Use in the "Post-Receive URLs" section of your GitHub repo.
 
if ( $_POST['payload'] ) {
  shell_exec( 'cd /home/nickie/www/tmp/testpub && git reset --hard HEAD && git pull' );
}
?>
git webhook, thanks