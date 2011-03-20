DATE=`date "+%Y-%m-%dT%H_%M_%S"`
TARGET=/home/nacho
BACKUPMNTPOINT=/mnt/backups
BACKUPROOT=$BACKUPMNTPOINT/leela$TARGET

RSYNCEXCLUDE=$HOME/.rsync/exclude
RSYNCOPTS="-azxP --delete --delete-excluded --exclude-from=$RSYNCEXCLUDE"

grep -q $BACKUPMNTPOINT /proc/mounts || exit -1

[ -e $BACKUPROOT ] || exit -1

function set_latest {
  mv $BACKUPROOT/in_progress-$DATE $BACKUPROOT/backup-$DATE
  echo "Setting latest backup to \"backup-$DATE\""
  ln -s $BACKUPROOT/backup-$DATE $BACKUPROOT/latest
}

if [ ! -e $BACKUPROOT/latest ]
then
  echo "It seems this is your first backup. It may take some time :("
  rsync $RSYNCOPTS $TARGET $BACKUPROOT/in_progress-$DATE
  if [ $? == 0 ]
  then
    set_latest
  fi
else
  echo "Old backup found in " `readlink $BACKUPROOT/latest`
  rsync $RSYNCOPTS --link-dest=$BACKUPROOT/latest $TARGET $BACKUPROOT/in_progress-$DATE
  if [ $? == 0 ]
  then
    rm $BACKUPROOT/latest
    set_latest
  fi
fi
